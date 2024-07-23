package com.be.rebook.domain.product.service;

import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.s3.model.ObjectMetadata;
import com.be.rebook.domain.product.dto.ProductRequestDTO;
import com.be.rebook.domain.product.repository.ProductRepository;
import com.be.rebook.domain.productImage.repository.ProductImageRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class ProductService {

    private final ProductImageRepository productImageRepository;
    private final AmazonS3Client amazonS3Client;
    private final ProductRepository productRepository;
//    private final MemberRopository memberRopository;

    /**
     *  S3
     */
    @Value("${cloud.aws.s3.bucket}")
    private String bucket;


    // TODO -> 이미지파일 업로드 후 product 엔티티 저장
    @Transactional
    public Long createProduct(Long memberId, ProductRequestDTO.ProductRequest productRequest, MultipartFile imageFile) throws IOException {

//        Member member = memberRepository.findById(memberId)
//                .orElseThrow(() -> new BaseException(ErrorCode.NOT_FOUND));

        // 원본 파일명
        String originalFileName = imageFile.getOriginalFilename();
        // 저장 파일명
        String storeFileName = createStoreFileName(originalFileName);

        // S3에 저장
        ObjectMetadata metadata = new ObjectMetadata();
        metadata.setContentType(imageFile.getContentType());
        metadata.setContentLength(imageFile.getSize());
        amazonS3Client.putObject(bucket, storeFileName, imageFile.getInputStream(), metadata);

//        Product product = productRepository.findById(productId)
//                .orElseThrow(() -> new BaseException(ErrorCode.NOT_EXIST_PRODUCT));


//        ProductImage productImage = ProductImage.builder()
//                .uploadFileName(originalFileName)
//                .storeFileName(storeFileName)
//                .product(product)
//                .build();
//
//        ProductImage savedProductImage = productImageRepository.save(productImage);
//        return savedProductImage.getId();
        return 1L;
    }

    /**
     * 파일명이 겹치는 것을 방지하기위해 중복되지않는 UUID를 생성해서 반환(ext는 확장자)
     */
    private String createStoreFileName(String originalFilename) {
        String ext = extractExt(originalFilename);
        String uuid = UUID.randomUUID().toString();
        return uuid + "." + ext;
    }

    /**
     * 파일 확장자를 추출하기 위해 만든 메서드
     */
    private String extractExt(String originalFilename) {
        int post = originalFilename.lastIndexOf(".");
        return originalFilename.substring(post + 1);
    }
}