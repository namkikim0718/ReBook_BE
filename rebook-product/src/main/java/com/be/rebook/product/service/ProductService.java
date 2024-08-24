package com.be.rebook.product.service;

import com.be.rebook.common.argumentresolver.auth.MemberLoginInfo;
import com.be.rebook.common.dto.PaginationResponseDTO;
import com.be.rebook.common.service.S3Service;
import com.be.rebook.common.type.S3FolderName;
import com.be.rebook.product.entity.Product;
import com.be.rebook.product.dto.ProductRequestDTO;
import com.be.rebook.product.repository.ProductRepository;
import com.be.rebook.product.entity.ProductImage;
import com.be.rebook.product.repository.ProductImageRepository;
import com.be.rebook.common.exception.BaseException;
import com.be.rebook.common.exception.ErrorCode;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;

import static com.be.rebook.product.dto.ProductResponseDTO.*;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Slf4j
public class ProductService {

    private final ProductImageRepository productImageRepository;
    private final ProductRepository productRepository;
    private final S3Service s3Service;


    @Transactional
    public Long createProduct(MemberLoginInfo memberLoginInfo,
                              ProductRequestDTO.ProductSaveRequestDTO productSaveRequestDTO,
                              List<MultipartFile> imageFiles) throws IOException {

        if (memberLoginInfo == null) {
            log.error("Unauthorized access attempt - memberLoginInfo is null");
            throw new BaseException(ErrorCode.UNAUTHORIZED);
        }
        String sellerUsername = memberLoginInfo.getUsername();
        // Product 먼저 저장
        Product product = Product.of(sellerUsername, productSaveRequestDTO);
        Long savedProductId = productRepository.save(product).getId();

        // 각 이미지 파일을 S3에 업로드 한 후 prductImage로 저장(Product에도 추가)
        for (MultipartFile imageFile : imageFiles) {

            String storeFileName = s3Service.uploadFile(imageFile, S3FolderName.PRODUCT);

            ProductImage productImage = ProductImage.builder()
                    .uploadFileName(imageFile.getOriginalFilename())
                    .storeFileName(storeFileName)
                    .product(product)
                    .build();

            productImageRepository.save(productImage);
        }

        return savedProductId;
    }

    /**
     * 상품 목록 조회
     */
    public PaginationResponseDTO<ProductListResponseDTO> findAllProductByFilter(ProductRequestDTO.ProductFilterDTO productFilterDTO) {

        log.info("dto 까보기");
        log.info("University: {}, Title: {}, Major: {}, MinPrice: {}, MaxPrice: {}",
                productFilterDTO.getUniversity(),
                productFilterDTO.getTitle(),
                productFilterDTO.getMajor(),
                productFilterDTO.getMinPrice(),
                productFilterDTO.getMaxPrice());

        Pageable pageable = PageRequest.of(productFilterDTO.getPage(), productFilterDTO.getSize());

        Page<ProductListResponseDTO> productPage = productRepository.findProductsByFilter(productFilterDTO, pageable)
                                                                 .map(ProductListResponseDTO::new);


        return new PaginationResponseDTO<>(productPage);
    }

    /**
     * 내가 쓴 글 조회
     */
    public List<ProductListResponseDTO> findAllMyProducts(MemberLoginInfo memberLoginInfo) {
        if (memberLoginInfo == null) {
            log.error("Unauthorized access attempt - memberLoginInfo is null");
            throw new BaseException(ErrorCode.UNAUTHORIZED);
        }

        List<Product> products = productRepository.findProductsBySellerUsername(memberLoginInfo.getUsername());

        productRepository.findById(1L);
        return products.stream()
                .map(ProductListResponseDTO::new)
                .collect(Collectors.toList());
    }

    /**
     * 상품 단건 조회
     */
    public ProductDetailResponseDTO findProductById(Long productId) {
        Product product = productRepository.findById(productId)
                .orElseThrow(() -> new BaseException(ErrorCode.NOT_EXIST_PRODUCT));

        return new ProductDetailResponseDTO(product);
    }

    /**
     * 상품 정보 수정
     */
    @Transactional
    public Long updateProduct(Long productId,
                              MemberLoginInfo memberLoginInfo,
                              ProductRequestDTO.ProductSaveRequestDTO productUpdateRequestDTO,
                              List<MultipartFile> imageFiles) throws IOException {

        if (memberLoginInfo == null) {
            log.error("Unauthorized access attempt - memberLoginInfo is null");
            throw new BaseException(ErrorCode.UNAUTHORIZED);
        }

        // 기존 상품 찾기
        Product existingProduct = productRepository.findById(productId)
                .orElseThrow(() -> new BaseException(ErrorCode.NOT_EXIST_PRODUCT));

        // 사용자가 이 상품의 소유자인지 확인
        if (!existingProduct.getSellerUsername().equals(memberLoginInfo.getUsername())) {
            log.error("Unauthorized access attempt - user is not the owner of the product");
            throw new BaseException(ErrorCode.UNAUTHORIZED);
        }

        // 상품 정보 업데이트
        existingProduct.updateProduct(productUpdateRequestDTO);

        // 기존 이미지 삭제 또는 유지 처리
        if (imageFiles != null && !imageFiles.isEmpty()) {
            // 기존 이미지를 모두 삭제
            List<ProductImage> existingImages = productImageRepository.findByProduct(existingProduct);
            for (ProductImage productImage : existingImages) {
                s3Service.deleteFile(S3FolderName.PRODUCT, productImage.getStoreFileName()); // S3에서 이미지 삭제
                productImageRepository.delete(productImage); // DB에서 이미지 삭제
            }

            // 새로운 이미지 업로드 및 저장
            for (MultipartFile imageFile : imageFiles) {
                String storeFileName = s3Service.uploadFile(imageFile, S3FolderName.PRODUCT);
                ProductImage productImage = ProductImage.builder()
                        .uploadFileName(imageFile.getOriginalFilename())
                        .storeFileName(storeFileName)
                        .product(existingProduct)
                        .build();
                productImageRepository.save(productImage);
            }
        }

        // 변경된 상품 저장
        return existingProduct.getId();
    }

    /**
     * 상품 상태 변경
     */
    @Transactional
    public Long changeStatus(MemberLoginInfo memberLoginInfo, Long productId, String status) {
        if (memberLoginInfo == null) {
            log.error("Unauthorized access attempt - memberLoginInfo is null");
            throw new BaseException(ErrorCode.UNAUTHORIZED);
        }

        Product product = productRepository.findById(productId)
                .orElseThrow(() -> new BaseException(ErrorCode.NOT_EXIST_PRODUCT));

        product.changeStatus(status);
        return productId;
    }

    /**
     * 상품 삭제
     */
    @Transactional
    public String deleteById(MemberLoginInfo memberLoginInfo, Long productId) {

        if (memberLoginInfo == null) {
            log.error("Unauthorized access attempt - memberLoginInfo is null");
            throw new BaseException(ErrorCode.UNAUTHORIZED);
        }

        Product product = productRepository.findById(productId)
                        .orElseThrow(() -> new BaseException(ErrorCode.NOT_EXIST_PRODUCT));

        List<String> fileNames = product.getProductImages().stream()
                                        .map(ProductImage::getStoreFileName)
                                        .collect(Collectors.toList());
        
        productRepository.deleteById(productId);

        fileNames.forEach(fileName -> {
            s3Service.deleteFile(S3FolderName.PRODUCT, fileName);
        });
        return "상품 삭제 완료";
    }
}