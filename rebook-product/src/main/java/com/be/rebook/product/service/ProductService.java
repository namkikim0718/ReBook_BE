package com.be.rebook.product.service;

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
    public Long createProduct(String sellerUsername,
                              ProductRequestDTO.ProductSaveRequestDTO productSaveRequestDTO,
                              List<MultipartFile> imageFiles) throws IOException {


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
    public List<ProductListResponseDTO> findAllProductByFilter(ProductRequestDTO.ProductFilterDTO productFilterDTO) {
        log.info("dto 까보기");
        log.info("University: {}, Title: {}, Major: {}, MinPrice: {}, MaxPrice: {}",
                productFilterDTO.getUniversity(),
                productFilterDTO.getTitle(),
                productFilterDTO.getMajor(),
                productFilterDTO.getMinPrice(),
                productFilterDTO.getMaxPrice());


        List<Product> products = productRepository.findProductsByFilter(productFilterDTO);


        return products.stream()
                .map(ProductListResponseDTO::new)
                .collect(Collectors.toList());
    }

    /**
     * 내가 쓴 글 조회
     */
    public List<ProductListResponseDTO> findAllMyProducts(String username) {
        List<Product> products = productRepository.findProductsBySellerUsername(username);

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
     * 상품 상태 변경
     */
    @Transactional
    public Long changeStatus(Long productId, String status) {
        Product product = productRepository.findById(productId)
                .orElseThrow(() -> new BaseException(ErrorCode.NOT_EXIST_PRODUCT));

        product.changeStatus(status);
        return productId;
    }

    /**
     * 상품 삭제
     */
    @Transactional
    public String deleteById(Long productId) {
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