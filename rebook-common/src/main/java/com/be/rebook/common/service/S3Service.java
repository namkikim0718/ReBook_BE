package com.be.rebook.common.service;

import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.s3.model.ObjectMetadata;
import com.be.rebook.common.type.S3FolderName;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class S3Service {

    private final AmazonS3Client amazonS3Client;

    @Value("${cloud.aws.s3.bucket}")
    private String bucket;

    /**
     * 이미지 파일 업로드
     * file -> 파일 자체를 매개변수로
     * folderName -> 해당 폴더 기입(Ex. "S3FolderName.PRODUCT")
     */
    public String uploadFile(MultipartFile file, S3FolderName folderName) throws IOException {
        // 원본 파일명
        String originalFileName = file.getOriginalFilename();
        // 저장 파일명
        String storeFileName = createStoreFileName(originalFileName);

        // S3에 저장될 전체 경로
        String fullPath = folderName.getFolderName() + "/" + storeFileName;

        // S3에 저장
        ObjectMetadata metadata = new ObjectMetadata();
        metadata.setContentType(file.getContentType());
        metadata.setContentLength(file.getSize());
        amazonS3Client.putObject(bucket, fullPath, file.getInputStream(), metadata);

        return storeFileName;
    }

    /**
     * S3 버킷에서 파일 삭제
     * folderName -> 해당 폴더이름 기입(Ex."S3FolderName.PRODUCT")
     * fileName -> 해당 이미지의 storeFileName을 넣으면 됨
     */
    public void deleteFile(S3FolderName folderName, String fileName) {
        String fullPath = folderName.getFolderName() + "/" + fileName;
        amazonS3Client.deleteObject(bucket, fullPath);
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
